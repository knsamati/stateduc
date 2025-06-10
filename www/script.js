document.addEventListener('DOMContentLoaded', function() {
            // Mobile menu toggle
            const hamburger = document.querySelector('.hamburger');
            const navLinks = document.querySelector('.nav-links');
            
            hamburger.addEventListener('click', function() {
                navLinks.classList.toggle('active');
            });

            // Smooth scrolling for anchor links
            document.querySelectorAll('a[href^="#"]').forEach(anchor => {
                anchor.addEventListener('click', function(e) {
                    e.preventDefault();
                    
                    const targetId = this.getAttribute('href');
                    if (targetId === '#') return;
                    
                    const targetElement = document.querySelector(targetId);
                    if (targetElement) {
                        window.scrollTo({
                            top: targetElement.offsetTop - 80,
                            behavior: 'smooth'
                        });
                        
                        // Close mobile menu if open
                        navLinks.classList.remove('active');
                    }
                });
            });

            // Testimonial slider
            const testimonials = document.querySelectorAll('.testimonial');
            const dots = document.querySelectorAll('.slider-dot');
            let currentSlide = 0;
            
            function showSlide(index) {
                testimonials.forEach(testimonial => testimonial.classList.remove('active'));
                dots.forEach(dot => dot.classList.remove('active'));
                
                testimonials[index].classList.add('active');
                dots[index].classList.add('active');
                currentSlide = index;
            }
            
            dots.forEach(dot => {
                dot.addEventListener('click', function() {
                    const slideIndex = parseInt(this.getAttribute('data-slide'));
                    showSlide(slideIndex);
                });
            });
            
            // Auto-rotate testimonials
            setInterval(() => {
                let nextSlide = (currentSlide + 1) % testimonials.length;
                showSlide(nextSlide);
            }, 5000);

            // Shiny integration points
            document.getElementById('shinyLogin').addEventListener('click', function(e) {
                e.preventDefault();
                // This would trigger Shiny to show the login modal
                if (window.Shiny) {
                    Shiny.setInputValue('show_login', Math.random());
                }
            });

            document.getElementById('shinyDemo').addEventListener('click', function(e) {
                e.preventDefault();
                // This would trigger Shiny to show the demo
                if (window.Shiny) {
                    Shiny.setInputValue('show_demo', Math.random());
                }
            });

            // Animation on scroll
            const animateOnScroll = function() {
                const elements = document.querySelectorAll('.feature-card, .section-title, .testimonial-slider');
                
                elements.forEach(element => {
                    const elementPosition = element.getBoundingClientRect().top;
                    const screenPosition = window.innerHeight / 1.3;
                    
                    if (elementPosition < screenPosition) {
                        element.style.opacity = '1';
                        element.style.transform = 'translateY(0)';
                    }
                });
            };
            
            // Set initial state for animated elements
            document.querySelectorAll('.feature-card, .section-title, .testimonial-slider').forEach(el => {
                el.style.opacity = '0';
                el.style.transform = 'translateY(20px)';
                el.style.transition = 'opacity 0.5s ease, transform 0.5s ease';
            });
            
            window.addEventListener('scroll', animateOnScroll);
            // Trigger once on load
            animateOnScroll();
        });

        // Function to integrate Shiny app
        function integrateShinyApp(html) {
            const container = document.getElementById('shinyAppContainer');
            if (container && html) {
                container.innerHTML = html;
                // You might need to adjust this based on how Shiny renders
            }
        }